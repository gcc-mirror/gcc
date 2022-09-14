! { dg-do compile }
! { dg-options "-O" }
subroutine s
   logical(1) :: a = .true.
   logical(2) :: b
   a = transfer(b, a)
end
