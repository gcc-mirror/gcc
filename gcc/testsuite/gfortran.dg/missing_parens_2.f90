! { dg-do run }
! PR34325 Wrong error message for syntax error
program aa
implicit none
real(kind=8)::r1=0
character(25) :: a
a = 'I am not a )))))'')''.'
if ((((((a /= "I am not a )))))')'.")))))) call abort
if ((((((a /= 'I am not a )))))'')''.')))))) call abort
a = "I am not a )))))"")""."
if ((((((a /= "I am not a )))))"")"".")))))) call abort
if (((3*r1)**2)>= 0) a = "good"
if (a /= "good") call abort
end
