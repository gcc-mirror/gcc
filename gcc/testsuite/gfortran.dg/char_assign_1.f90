! { dg-do run }
! { dg-options "-Wcharacter-truncation" }
! Tests the fix for PR31266: references to CHARACTER
! components lead to the wrong length being assigned to substring
! expressions.
type data
   character(len=5) :: c
end type data
type(data), dimension(5), target :: y
character(len=2), dimension(5) :: p
character(len=3), dimension(5) :: q

y(:)%c = "abcdef" ! { dg-warning "in assignment \\(5/6\\)" }
p(1) = y(1)%c(3:) ! { dg-warning "in assignment \\(2/3\\)" }
if (p(1).ne."cd") call abort()

p(1) = y(1)%c  ! { dg-warning "in assignment \\(2/5\\)" }
if (p(1).ne."ab") call abort()

q = "xyz"
p = q ! { dg-warning "CHARACTER expression will be truncated in assignment \\(2/3\\)" }
if (any (p.ne.q(:)(1:2))) call abort()
end
