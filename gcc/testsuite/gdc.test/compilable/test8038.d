template t(T){alias T t;}

t!(#line 10
    t!(
        int,
    )
) i;

t!(
    t!(#line 10
        int,
    )
) j;
