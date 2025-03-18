extern crate datafrog;
use datafrog::Iteration;

fn main() {
    let timer = ::std::time::Instant::now();

    // Make space for input data.
    let mut nodes = Vec::new();
    let mut edges = Vec::new();

    // Read input data from a handy file.
    use std::fs::File;
    use std::io::{BufRead, BufReader};

    let filename = std::env::args().nth(1).unwrap();
    let file = BufReader::new(File::open(filename).unwrap());
    for readline in file.lines() {
        let line = readline.expect("read error");
        if !line.is_empty() && !line.starts_with('#') {
            let mut elts = line[..].split_whitespace();
            let src: u32 = elts.next().unwrap().parse().expect("malformed src");
            let dst: u32 = elts.next().unwrap().parse().expect("malformed dst");
            let typ: &str = elts.next().unwrap();
            match typ {
                "n" => {
                    nodes.push((dst, src));
                }
                "e" => {
                    edges.push((src, dst));
                }
                unk => panic!("unknown type: {}", unk),
            }
        }
    }

    println!("{:?}\tData loaded", timer.elapsed());

    // Create a new iteration context, ...
    let mut iteration = Iteration::new();

    // .. some variables, ..
    let variable1 = iteration.variable::<(u32, u32)>("nodes");
    let variable2 = iteration.variable::<(u32, u32)>("edges");

    // .. load them with some initial values, ..
    variable1.insert(nodes.into());
    variable2.insert(edges.into());

    // .. and then start iterating rules!
    while iteration.changed() {
        // N(a,c) <-  N(a,b), E(b,c)
        variable1.from_join(&variable1, &variable2, |_b, &a, &c| (c, a));
    }

    let reachable = variable1.complete();

    println!(
        "{:?}\tComputation complete (nodes_final: {})",
        timer.elapsed(),
        reachable.len()
    );
}
