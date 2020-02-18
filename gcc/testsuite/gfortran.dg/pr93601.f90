! { dg-do compile }

program p
   class(*), allocatable :: z
   z = z'1' ! { dg-error "BOZ literal constant at" }
end

