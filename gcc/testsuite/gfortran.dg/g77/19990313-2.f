c { dg-do run }
       integer(kind=8) foo, bar
       complex c
        data c/(4e10,0)/
        foo = 4e10
        bar = c
        if (foo .ne. bar) call abort
        end
