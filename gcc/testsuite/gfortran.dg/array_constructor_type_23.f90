! { dg-do compile }
! PR 83999 - this used to ICE
! Origial test case by Gerhard Steinmetz

program p
        character(2) :: c = 'a' // [character :: [1]] ! { dg-error "Illegal type in character concatenation" }
end
