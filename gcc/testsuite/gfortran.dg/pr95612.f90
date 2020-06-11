! { dg-do compile }

program p
   integer, pointer :: y(:) => shape(1)   ! { dg-error "Zero-sized array detected at .1. where an entity with the TARGET attribute is expected" }
   integer, pointer :: z(:) => shape([1]) ! { dg-error "Pointer assignment target in initialization expression does not have the TARGET attribute at .1." }
end

