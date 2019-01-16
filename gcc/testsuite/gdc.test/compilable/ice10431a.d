mixin ADT!();

struct Tuple(TL...) { TL expand; }

template Seq(T...) { alias T Seq; }

template ADT()
{
    mixin(q{
        struct ListI
        {
            private
            {
                size_t tag;
                union { Seq!(Tuple!()*, Tuple!(int,ListI,)*,) data; }
            }
        }
    });
}
