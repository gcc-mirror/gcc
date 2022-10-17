! { dg-do compile }

subroutine g
   call s([1])
end
subroutine h(x)
   integer, pointer :: x(..)
   call s(x) ! { dg-error "Assumed-rank argument requires an explicit interface" } 
end
