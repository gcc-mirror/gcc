c { dg-do compile }
c
c  g77 used to warn for this case
c 19990218-1.f: In program `test':
c 19990218-1.f:13: 
c             double precision function fun(a,b)
c                                       1
c  19990218-1.f:23: (continued):
c             c=fun(a,b)
c               2
c  Global name `fun' at (2) has different type at (1) [info -f g77 M GLOBALS]
c
        double precision function fun(a,b)
        double precision a,b
        print*,'in sub: a,b=',a,b
        fun=a*b
        print*,'in sub: fun=',fun
        return
        end
        program test
        double precision a,b,c
        data a,b/1.0d-46,1.0d0/
        c=fun(a,b)
        print*,'in main: fun=',c
        end
