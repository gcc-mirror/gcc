// { dg-do assemble  }

template<int i> int f (void)
{
        if (__extension__ ({ 1; }))
                return 0;
        return 1;
}

void g (void)
{
        f<1> ();
}
