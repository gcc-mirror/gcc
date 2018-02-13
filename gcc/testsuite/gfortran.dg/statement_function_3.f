! { dg-do compile }
! PR fortran/35299
      subroutine phtod(e,n,i,h)
      dimension e(n)
      hstar(e,b)=b**.4*((1.25*fun(-e/40)+.18)) ! { dg-error "must be scalar" }
      a = 1.
      h = hstar(e(i-1), a)
      end

      function fun(a)
         real a(*)
         fun = 42
      end
! { dg-prune-output " Obsolescent feature" }

