! { dg-do run }
! PR31495 Is this continuation legal? 
program print_ascertain
character (len=50) :: str
str = "hello world &
&    &
&!"
if (str.ne."hello world     !") call abort
end program print_ascertain
