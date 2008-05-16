! { dg-do compile }
! PR34325 Wrong error message for syntax error
program aa
implicit none
real(kind=8)::r1=0
real(kind=8),dimension((1)::r2 ! { dg-error "Missing '\\)' in statement" }
real(kind=8),dimension(3,3)::r3
character(25) :: a
a = 'I am not a )))))'')''.'
if ((((((a /= "I am not a )))))')'.")))))) call abort
if ((((((a /= 'I am not a )))))'')''.')))))) call abort
a = "I am not a )))))"")""."
if ((((((a /= "I am not a )))))"")"".")))))) call abort
if (((3*r1)**2)>= 0) a = "good"
if ((3*r1)**2)>= 0) a = "bad" ! { dg-error "Missing '\\(' in statement" }
r3((2,2)) = 4.3 ! { dg-error "found COMPLEX" }
do while ((.true.) ! { dg-error "Missing '\\)' in statement" }
do while (.true. ! { dg-error "Missing '\\)' in statement" }
end
