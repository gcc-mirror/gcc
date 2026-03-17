! { dg-do compile }
! PR fortran/124208

subroutine nested_forall_in_associate
  implicit none
  integer :: lane
  integer :: box(6)

  box = 0
  do concurrent (lane = 1:4)
    associate (idx => lane)
      forall (idx = 1:3, box(idx) == 0)
        box(mod (idx + lane - 1, 3) + 1) = idx + lane
      end forall
    end associate
  end do
end subroutine nested_forall_in_associate

subroutine shadowed_do_concurrent_blocks
  implicit none
  integer :: i
  integer :: acc(8)

  acc = 0
  block
    integer :: i
    i = 2
    do concurrent (i = 1:3)
      associate (base => i)
      block
        integer :: i
        i = base + 1
        do concurrent (i = base + 1:5)
          acc(i) = acc(i) + i
        end do
      end block
      end associate
    end do
  end block
end subroutine shadowed_do_concurrent_blocks
