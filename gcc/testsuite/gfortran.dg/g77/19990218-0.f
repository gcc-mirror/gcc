c { dg-do compile }
        program test
        double precision a,b,c
        data a,b/1.0d-46,1.0d0/
        c=fun(a,b)
        print*,'in main: fun=',c
        end
        double precision function fun(a,b)
        double precision a,b
        print*,'in sub: a,b=',a,b
        fun=a*b
        print*,'in sub: fun=',fun
        return
        end
