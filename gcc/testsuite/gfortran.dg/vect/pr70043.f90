! { dg-do compile  }
! { dg-additional-options "-Ofast -g" }
! { dg-additional-options "-march=haswell" { target i?86-*-* x86_64-*-* } }

subroutine fn1(a, b)
  real(8), intent(in) ::  b(100)
  real(8), intent(inout) :: a(100)
  real(8) c
  do i=0,100
     if( a(i) < 0.0 ) then
        c =  a(i) * b(i)
        a(i) = a(i) - c / b(i)
     endif
  enddo
end subroutine fn1
