! { dg-do run }
! PR20713. Pad and truncate string.

character(len = 6),parameter:: a = 'hello'
character(len = 6),parameter:: b = 'hello *'
character(len = 6),parameter:: c (1:1) = 'hello'
character(len = 11) line

write (line, '(6A)') a, 'world'
if (line .ne. 'hello world') STOP 1

write (line, '(6A)') b, 'world'
if (line .ne. 'hello world') STOP 2

write (line, '(6A)') c, 'world'
if (line .ne. 'hello world') STOP 3

write (line, '(6A)') c(1), 'world'
if (line .ne. 'hello world') STOP 4
end
