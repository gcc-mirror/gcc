! { dg-do run }
! PR70592 dynamically-allocated character array
! Contributed by Peter Knowles <KnowlesPJ@Cardiff.ac.uk>
!
PROGRAM main
 character(len=7) :: res
 CHARACTER(len=:), DIMENSION(:), POINTER :: cp
 INTEGER :: i
 ALLOCATE(CHARACTER(len=1) :: cp(1:6))
 if (SIZE(cp) /= 6 .or. LBOUND(cp,1) /= 1 .or. UBOUND(cp,1) /= 6) call abort()
 cp(1)='1'
 cp(2)='2'
 cp(3)='3'
 cp(4)='4'
 cp(5)='5'
 cp(6)='6'
 write (res, *) cp
 if (res /= ' 123456') call abort()
END PROGRAM main
