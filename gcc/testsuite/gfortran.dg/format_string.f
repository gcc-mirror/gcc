c { dg-do compile }
c PR fortran/50407
c
      program bar

      interface operator (.ip.)
        function mul (i1, i2)
          character(20) mul
          intent(in) :: i1,i2
        end function
      end interface

      character(20) foo
      i=3
      j=4
      print 2.ip.8  ! compiles fine 
      print i.ip.2  ! compiles fine 
      print i.ip.j  ! compiles fine
      foo = 1_'(I0,I4.4)'
      print foo, i,j
      print 1_'(I0,1X,I4.4)', i, j
      end

      function mul (i1, i2)
        character(20) mul
        intent(in) :: i1,i2
        integer prod
        prod=i1*i2
        write(mul,100) prod
100     format("('ok ",i2,"')")
      end function
