c { dg-do run }
        integer(kind=8) foo, bar
        complex(kind=8) c
        data c/(4d10,0)/
        foo = 4d10
        bar = c
        if (foo .ne. bar) STOP 1
        end
