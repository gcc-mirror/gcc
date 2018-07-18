c { dg-do run }
        integer(kind=8) foo, bar
        double precision r
        data r/4d10/
        foo = 4d10
        bar = r
        if (foo .ne. bar) STOP 1
        end
