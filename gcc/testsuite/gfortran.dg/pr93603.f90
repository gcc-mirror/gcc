! { dg-do compile }

program p
  associate (y => z'1') ! { dg-error "cannot be a BOZ literal constant" }
  end associate         ! { dg-error "Expecting END PROGRAM" }
end

