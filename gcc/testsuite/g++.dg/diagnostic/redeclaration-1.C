void
foo (int i) // { dg-message "10:.int i. previously declared here" }
{
  int i  // { dg-error "7:declaration of .int i. shadows a parameter" }
    (0);
  
  for (int j ;;)  // { dg-message "12:.int j. previously declared here" }
    int j  // { dg-error "9:redeclaration of .int j." }
      (0);
}

void
bar (int i)  // { dg-message "10:.int i. previously declared here" }
  try
    { }
  catch (...)
    {
      int i  // { dg-error "11:declaration of .int i. shadows a parameter" }
	(0);
    }
