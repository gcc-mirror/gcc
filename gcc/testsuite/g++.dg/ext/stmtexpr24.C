// { dg-do compile }
// { dg-options "" }

void
foo (int x)
{
  bool a = false;
  if (x == 1)
    goto l1;						// { dg-message "from here" }
  a = ({ l0:; if (x == 0) goto l0; true; });
  a = ({ if (x == 0) throw 1; true; });
  a = ({ l1:; true; });					// { dg-error "jump to label 'l1'" }
							// { dg-message "enters statement expression" "" { target *-*-* } .-1 }
  a = ({ l2:; true; });					// { dg-error "jump to label 'l2'" }
  switch (x)
    {
    case 2:
      a = ({ case 3:; true; });				// { dg-error "jump to case label" }
							// { dg-message "enters statement expression" "" { target *-*-* } .-1 }
      a = ({ default:; true; });			// { dg-error "jump to case label" }
							// { dg-message "enters statement expression" "" { target *-*-* } .-1 }
      break;
    }
  if (x == 4)
    goto l2;						// { dg-message "from here" }
							// { dg-message "enters statement expression" "" { target *-*-* } .-1 }
}
