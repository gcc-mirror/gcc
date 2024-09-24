! { dg-do run }
! { dg-options "-funsigned" }

! The leading bytes of the unsigned sequences should be the same for
! kinds 1 to 8.  This also tests array I/O for unsigneds.

program memain
  implicit none
  integer, dimension(:), allocatable :: seed
  integer :: n
  call random_seed (size=n)
  allocate(seed(n))
  call test1
  call test2
contains
  subroutine test1
    unsigned(1) :: u1
    unsigned(2) :: u2
    unsigned(4) :: u4
    unsigned(8) :: u8
    character (len=16) :: line1, line2, line4, line8
    integer :: i, n
    do i=1,10
       call random_seed(get=seed)
       call random_number(u1)
       write (line1,'(Z2.2)') u1
       call random_seed(put=seed)
       call random_number(u2)
       write (line2,'(Z4.4)') u2
       call random_seed(put=seed)
       call random_number(u4)
       write (line4,'(Z8.8)') u4
       call random_seed(put=seed)
       call random_number(u8)
       write (line8,'(Z16.16)') u8
       if (line8(1:8) /= line4 (1:8)) error stop 1
       if (line4(1:4) /= line2 (1:4)) error stop 2
       if (line2(1:2) /= line1 (1:2)) error stop 3
    end do
  end subroutine test1
  subroutine test2
    unsigned(1), dimension(2,2) :: v1
    unsigned(2), dimension(2,2) :: v2
    unsigned(4), dimension(2,2) :: v4
    unsigned(8), dimension(2,2) :: v8
    character(len=16), dimension(4) :: c1, c2, c4, c8
    call random_seed(put=seed)
    call random_number (v1)
    write (c1,'(Z2.2)') v1
    call random_seed(put=seed)
    call random_number (v2)
    write (c2,'(Z4.4)') v2
    call random_seed(put=seed)
    call random_number (v4)
    write (c4,'(Z8.8)') v4
    call random_seed(put=seed)
    call random_number (v8)
    write (c8,'(Z16.16)') v8
    if (any(c8(:)(1:8) /= c4(:)(1:8))) error stop 10
    if (any(c4(:)(1:4) /= c2(:)(1:4))) error stop 11
    if (any(c2(:)(1:2) /= c1(:)(1:2))) error stop 12
  end subroutine test2
end program memain
