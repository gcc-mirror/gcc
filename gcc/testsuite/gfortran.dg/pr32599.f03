! { dg-do compile }
! PR fortran/32599
! Verifies that character string arguments to a bind(c) procedure have length 
! 1, or no len is specified.  
module pr32599
  interface
     subroutine destroy(path) BIND(C) ! { dg-error "must be length 1" }
       use iso_c_binding
       implicit none
       character(len=*,kind=c_char), intent(IN) :: path 
     end subroutine destroy

     subroutine create(path) BIND(C) ! { dg-error "must be length 1" }
       use iso_c_binding
       implicit none
       character(len=5,kind=c_char), intent(IN) :: path 
     end subroutine create

     ! This should be valid.
     subroutine create1(path) BIND(C)
       use iso_c_binding
       implicit none
       character(len=1,kind=c_char), intent(IN) :: path 
     end subroutine create1

     ! This should be valid.
     subroutine create2(path) BIND(C)
       use iso_c_binding
       implicit none
       character(kind=c_char), intent(IN) :: path
     end subroutine create2

     ! This should be valid.
     subroutine create3(path) BIND(C)
       use iso_c_binding
       implicit none
       character(kind=c_char), dimension(*), intent(IN) :: path
     end subroutine create3
  end interface
end module pr32599
