MODULE badxproc ;

TYPE xProc = PROCEDURE(): BOOLEAN;
VAR x: xProc;

BEGIN
  IF x = 0 THEN END;
END badxproc.
