implicit integer(a), logical(b-c), real(d-y), integer(z)
a = 1_4
b = .true.
c = b
d = 1.0e2
y = d
z = a
end
! test prompted by PR 16161
! we used to match "character (c)" wrongly in the below, confusing the parser
subroutine b
implicit character (c)
end
