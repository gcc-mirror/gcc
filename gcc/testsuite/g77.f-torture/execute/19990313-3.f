        integer *8 foo, bar
	double complex c
        data c/(4d10,0)/
        foo = 4d10
        bar = c
        if (foo .ne. bar) call abort
        end
