! Originally contributed by Tobias Burnas.
! { dg-do compile { target { powerpc*-*-* } } }
! { dg-require-effective-target powerpc_p9vector_ok }
! { dg-options "-mdejagnu-cpu=405 -mpower9-minmax -mfloat128-type" }
! { dg-excess-errors "expect error due to conflicting target options" }
! Since the error message is not associated with a particular line
! number, we cannot use the dg-error directive and cannot specify a
! regexp to describe the expected error message.  The expected error
! message is:
!  "Power9 target option is incompatible with -mcpu=<xxx> for <xxx>
!    less than power9"

program stream_test
    implicit none
    character(len=*), parameter :: rec1 = 'record1'
    character(len=*), parameter :: rec2 = 'record2'
    character(len=50) :: str1,str2
    integer           :: len, i
    real              :: r

    open(10,form='formatted',access='stream',&
         status='scratch',position='rewind')
    write(10,'(a)') rec1//new_line('a')//rec2
    rewind(10)
    read(10,*) str1
    read(10,*) str2
    if(str1 /= rec1 .or. str2 /= rec2) STOP 1
    rewind(10)
    read(10,'(a)') str1
    read(10,'(a)') str2
    if(str1 /= rec1 .or. str2 /= rec2) STOP 2
    close(10)

    open(10,form='formatted',access='stream',&
         status='scratch',position='rewind')
    write(10,*) '123 '//trim(rec1)//'  1e-12'
    write(10,*) '12345.6789'
    rewind(10)
    read(10,*) i,str1
    read(10,*) r
    if(i /= 123 .or. str1 /= rec1 .or. r /= 12345.6789) &
      STOP 3
    close(10)

    open(unit=10,form='unformatted',access='stream', &
         status='scratch',position='rewind')
    write(10) rec1//new_line('a')//rec2
    len = len_trim(rec1//new_line('a')//rec2)
    rewind(10)
    read(10) str1(1:len)
    if(str1 /= rec1//new_line('a')//rec2) STOP 4
end program stream_test
