! { dg-do compile }
! Code contributed by Gerhard Steinmetz
program p
   call q
   call r
end program p

subroutine q
   print *, -merge([3,4], 0, [.false.,.true.])
end

subroutine r
   print *, 2 + merge([3,4], 0, [.false.,.true.])
end
