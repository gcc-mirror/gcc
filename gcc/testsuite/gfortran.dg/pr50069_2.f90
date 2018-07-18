! { dg-do compile }

function reverse(string)
implicit none
character(len=*), intent(in) :: string
character(len=:),allocatable :: reverse
integer i
reverse = string
forall (i=1:len(reverse)) reverse(i:i) = &
  reverse(len(reverse)-i+1:len(reverse)-i+1)
end function reverse
