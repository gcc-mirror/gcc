! { dg-do run }
! { dg-additional-sources ISO_Fortran_binding_6.c }
!
! Test fix of PR89366.
!
! Contributed by Reinhold Bader  <Bader@lrz.de>
!
program assumed_length_01
  use, intrinsic :: iso_c_binding
  implicit none
  integer, parameter :: strlen = 12
  integer(c_int) :: ierr(3)
  character(kind=c_char,len=strlen) :: s1
  character(kind=c_char,len=:), allocatable :: s2
  character(kind=c_char,len=:), pointer :: s3
!
! invoke a C function that processes an assumed length string
  interface
     subroutine process_string(this, ierr) BIND(C)
       import :: c_char, c_int
       character(kind=c_char,len=*), intent(in) :: this(..)
       integer(c_int), intent(inout) :: ierr
     end subroutine process_string
  end interface
!
!
  ierr = 0
  s1 = c_char_'wrzlprmft' // c_null_char
  call process_string(s1, ierr(1))
  if (ierr(1) /= 0) stop 1
  s2 = c_char_'wrzlprmft' // c_null_char
  allocate(s3, source=trim(s1))
  call process_string(s2, ierr(2))
  if (ierr(2) /= 0) stop 2
  call process_string(s3, ierr(3))
  if (ierr(3) /= 0) stop 3
  if (sum(abs(ierr)) == 0) write(*,*) 'OK'

  deallocate(s2,s3)

end program assumed_length_01
