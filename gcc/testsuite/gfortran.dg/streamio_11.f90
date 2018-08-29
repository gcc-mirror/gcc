! { dg-do run }
! PR29277 Stream IO test 11, tests formatted form.
! Contributed by Tobias Burnas.
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
