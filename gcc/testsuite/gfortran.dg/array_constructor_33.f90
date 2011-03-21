! { dg-do compile }
! { dg-timeout-factor 4 }
! PR20923 gfortran slow for large array constructors.
! Test case prepared from PR by Jerry DeLisle <jvdelisle@gcc.gnu.org>
program sel
    implicit none
    integer(kind=4),parameter      :: n=1000
    integer(kind=4)                :: i,j
    real(kind=4),dimension(n*n)    :: vect
    vect(:) = (/ ((( (i+j+3)),i=1,n),j=1,n) /)
end
