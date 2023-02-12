! { dg-do run }
! { dg-additional-sources read_dir-aux.c }
!
! PR67367

program bug
   use iso_c_binding
   implicit none

   interface
     integer(c_int) function expect_open_to_fail () bind(C)
       import
     end
     subroutine my_verify_not_exists(s) bind(C)
       ! Aborts if the passed pathname (still) exists
       import
       character(len=1,kind=c_char) :: s(*)
     end subroutine
     subroutine my_mkdir(s) bind(C)
       ! Call POSIX's mkdir - and ignore fails due to
       ! existing directories but fail otherwise
       import
       character(len=1,kind=c_char) :: s(*)
     end subroutine
     subroutine my_rmdir(s) bind(C)
       ! Call POSIX's rmdir - and ignore fails
       import
       character(len=1,kind=c_char) :: s(*)
     end subroutine
   end interface

   character(len=*), parameter :: sdir = "junko.dir"
   character(len=*,kind=c_char), parameter :: c_sdir = sdir // c_null_char

   character(len=1) :: c
   integer ios

   if (expect_open_to_fail () /= 0) then
      ! Windows is documented to fail with EACCESS when trying to open a
      ! directory. However, target macros such as __WIN32__ are not defined
      ! in Fortran; hence, we use a detour via this C function.
      ! Check for '.' which is a known-to-exist directory:
      open(unit=10, file='.',iostat=ios,action='read',access='stream')
      if (ios == 0) &
          stop 3  ! Error: open to fail (EACCESS)
       stop 0  ! OK
   endif

   call my_mkdir(c_sdir)
   open(unit=10, file=sdir,iostat=ios,action='read',access='stream')

   if (ios.ne.0) then
      call my_rmdir(c_sdir)
      STOP 1
   end if
   read(10, iostat=ios) c
   if (ios.ne.21.and.ios.ne.0) then  ! EISDIR has often the value 21
      close(10, status='delete')
      call my_verify_not_exists(c_sdir)
      STOP 2
   end if
   close(10, status='delete')
   call my_verify_not_exists(c_sdir)
end program bug
