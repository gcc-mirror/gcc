! { dg-do compile }
subroutine test2(s)
integer(1) :: i
integer (8) :: s

do i = 10, HUGE(i) - 10, 222 ! { dg-error "overflow converting" }
  s = s + 1
end do

end subroutine test2
