! { dg-do compile }
! Tests the fix for PR27698, where names not starting with a letter were
! rejected but not diagnosed with a proper message.
SUBROUTINE _foo ! { dg-error "Invalid character in name" }
END

