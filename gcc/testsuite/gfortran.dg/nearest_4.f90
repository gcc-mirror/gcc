! { dg-do compile }
! PR33296 nearest(huge(1.0),1.0) gives an error
real x
x = nearest(-huge(1.0),-1.0)
print *, x
end
