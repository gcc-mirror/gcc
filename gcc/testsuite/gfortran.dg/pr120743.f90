! { dg-do compile }
! PR fortran/120743 - ICE in verify_gimple_in_seq with substrings
!
! Testcase as reduced by Jerry DeLisle 

module what
  implicit none
  CHARACTER(LEN=:), ALLOCATABLE :: attrlist
contains
  SUBROUTINE get_c_attr ( attrname, attrval_c )
    !
    ! returns attrval_c='' if not found
    !
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: attrname
    CHARACTER(LEN=*), INTENT(OUT) :: attrval_c
    !
    CHARACTER(LEN=1) :: quote
    INTEGER :: j0, j1
    LOGICAL :: found
    !
    ! search for attribute name in attrlist: attr1="val1" attr2="val2" ...
    !
    attrval_c = ''
    if ( .not. allocated(attrlist) ) return
    if ( len_trim(attrlist) < 1 ) return
    !
    j0 = 1
    do while ( j0 < len_trim(attrlist) )
       ! locate = and first quote
       j1 = index ( attrlist(j0:), '=' )
       quote = attrlist(j0+j1:j0+j1)
       ! next line: something is not right
       if ( quote /= '"' .and. quote /= "'" ) return
    end do
    !
  END SUBROUTINE get_c_attr
end module what
