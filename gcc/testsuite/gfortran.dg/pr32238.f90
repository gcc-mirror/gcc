!PR fortran/32238
! { dg-do compile }
! { dg-final { cleanup-modules "bug_test" } }

module bug_test

contains
  subroutine bug(c)

  implicit none

  integer, parameter :: fp = selected_real_kind(13)
  complex(kind=fp)              :: c(:,:)
  where( abs( aimag( c ) ) < 1.e-10_fp )                             &
  &    c = cmplx( real( c , fp ) , 0._fp , fp )
  where( abs( real( c , fp ) ) < 1.e-10_fp )                         &
  &    c = cmplx( 0._fp , aimag( c ) , fp )

  return
  end subroutine bug

end module bug_test
