! { dg-do compile }
! { dg-options "-Ofast" }
module m
  implicit none
  private
  save

  integer, parameter, public :: &
    ii4          = selected_int_kind(6), &
    rr8          = selected_real_kind(13)

  integer (ii4), dimension(40,40,199), public :: xyz
  public :: foo
contains
  subroutine foo(a)
    real (rr8), dimension(40,40), intent(out) :: a
    real (rr8), dimension(40,40) :: b
    integer (ii4), dimension(40,40) :: c
    integer  i, j

    j = 10
    do i=11,30
       b(i,j) = 123 * a(i,j) + 34 * a(i,j+1) &
            + 34 * a(i,j-1) + a(i+1,j+1) &
            + a(i+1,j-1) + a(i-1,j+1) &
            + a(i-1,j-1)
       c(i,j) = 123
    end do

    where ((xyz(:,:,2) /= 0) .and. (c /= 0))
      a = b/real(c)
    elsewhere
      a = 456
    endwhere
 end subroutine foo
end module m
