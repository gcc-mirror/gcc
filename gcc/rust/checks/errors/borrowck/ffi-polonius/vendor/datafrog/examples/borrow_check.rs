extern crate datafrog;
use datafrog::Iteration;

type Region = u32;
type Borrow = u32;
type Point = u32;

fn main() {
    let subset = {
        // Create a new iteration context, ...
        let mut iteration1 = Iteration::new();

        // .. some variables, ..
        let subset = iteration1.variable::<(Region, Region, Point)>("subset");

        // different indices for `subset`.
        let subset_r1p = iteration1.variable::<((Region, Point), Region)>("subset_r1p");
        let subset_r2p = iteration1.variable::<((Region, Point), Region)>("subset_r2p");
        let subset_p = iteration1.variable::<(Point, (Region, Region))>("subset_p");

        // temporaries as we perform a multi-way join.
        let subset_1 = iteration1.variable::<((Region, Point), Region)>("subset_1");
        let subset_2 = iteration1.variable::<((Region, Point), Region)>("subset_2");

        let region_live_at = iteration1.variable::<((Region, Point), ())>("region_live_at");
        let cfg_edge_p = iteration1.variable::<(Point, Point)>("cfg_edge_p");

        // load initial facts.
        subset.insert(Vec::new().into());
        region_live_at.insert(Vec::new().into());
        cfg_edge_p.insert(Vec::new().into());

        // .. and then start iterating rules!
        while iteration1.changed() {
            // remap fields to re-index by keys.
            subset_r1p.from_map(&subset, |&(r1, r2, p)| ((r1, p), r2));
            subset_r2p.from_map(&subset, |&(r1, r2, p)| ((r2, p), r1));
            subset_p.from_map(&subset, |&(r1, r2, p)| (p, (r1, r2)));

            // R0: subset(R1, R2, P) :- outlives(R1, R2, P).
            // Already loaded; outlives is static.

            // R1: subset(R1, R3, P) :-
            //   subset(R1, R2, P),
            //   subset(R2, R3, P).
            subset.from_join(&subset_r2p, &subset_r1p, |&(_r2, p), &r1, &r3| (r1, r3, p));

            // R2: subset(R1, R2, Q) :-
            //   subset(R1, R2, P),
            //   cfg_edge(P, Q),
            //   region_live_at(R1, Q),
            //   region_live_at(R2, Q).

            subset_1.from_join(&subset_p, &cfg_edge_p, |&_p, &(r1, r2), &q| ((r1, q), r2));
            subset_2.from_join(&subset_1, &region_live_at, |&(r1, q), &r2, &()| {
                ((r2, q), r1)
            });
            subset.from_join(&subset_2, &region_live_at, |&(r2, q), &r1, &()| (r1, r2, q));
        }

        subset_r1p.complete()
    };

    let _requires = {
        // Create a new iteration context, ...
        let mut iteration2 = Iteration::new();

        // .. some variables, ..
        let requires = iteration2.variable::<(Region, Borrow, Point)>("requires");
        requires.insert(Vec::new().into());

        let requires_rp = iteration2.variable::<((Region, Point), Borrow)>("requires_rp");
        let requires_bp = iteration2.variable::<((Borrow, Point), Region)>("requires_bp");

        let requires_1 = iteration2.variable::<(Point, (Borrow, Region))>("requires_1");
        let requires_2 = iteration2.variable::<((Region, Point), Borrow)>("requires_2");

        let subset_r1p = iteration2.variable::<((Region, Point), Region)>("subset_r1p");
        subset_r1p.insert(subset);

        let killed = Vec::new().into();
        let region_live_at = iteration2.variable::<((Region, Point), ())>("region_live_at");
        let cfg_edge_p = iteration2.variable::<(Point, Point)>("cfg_edge_p");

        // .. and then start iterating rules!
        while iteration2.changed() {
            requires_rp.from_map(&requires, |&(r, b, p)| ((r, p), b));
            requires_bp.from_map(&requires, |&(r, b, p)| ((b, p), r));

            // requires(R, B, P) :- borrow_region(R, B, P).
            // Already loaded; borrow_region is static.

            // requires(R2, B, P) :-
            //   requires(R1, B, P),
            //   subset(R1, R2, P).
            requires.from_join(&requires_rp, &subset_r1p, |&(_r1, p), &b, &r2| (r2, b, p));

            // requires(R, B, Q) :-
            //   requires(R, B, P),
            //   !killed(B, P),
            //   cfg_edge(P, Q),
            //   (region_live_at(R, Q); universal_region(R)).

            requires_1.from_antijoin(&requires_bp, &killed, |&(b, p), &r| (p, (b, r)));
            requires_2.from_join(&requires_1, &cfg_edge_p, |&_p, &(b, r), &q| ((r, q), b));
            requires.from_join(&requires_2, &region_live_at, |&(r, q), &b, &()| (r, b, q));
        }

        requires.complete()
    };

    // borrow_live_at(B, P) :- requires(R, B, P), region_live_at(R, P)

    // borrow_live_at(B, P) :- requires(R, B, P), universal_region(R).
}
