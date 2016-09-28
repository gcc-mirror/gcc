program p
   integer :: i = 0
   integer :: z(2)
   data z /2*i/ ! { dg-error "must be a PARAMETER in DATA" }
end
