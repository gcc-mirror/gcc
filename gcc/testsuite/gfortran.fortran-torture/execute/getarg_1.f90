! Check that getarg does somethig sensible.
program getarg_1
  CHARACTER*10 ARGS, ARGS2
  INTEGER*4 I
  INTEGER*2 I2
  I = 0
  CALL GETARG(I,ARGS)
  ! This should return the invoking command.  The actual value depends 
  ! on the OS, but a blank string is wrong no matter what.
  ! ??? What about deep embedded systems?

  I2 = 0
  CALL GETARG(I2,ARGS2)
  if (args2.ne.args) STOP 1

  if (args.eq.'') STOP 2
  I = 1
  CALL GETARG(I,ARGS)
  if (args.ne.'') STOP 3
  I = -1
  CALL GETARG(I,ARGS)
  if (args.ne.'') STOP 4
  ! Assume we won't have been called with more that 4 args.
  I = 4
  CALL GETARG(I,ARGS)
  if (args.ne.'') STOP 5
  I = 1000
  CALL GETARG(I,ARGS)
  if (args.ne.'') STOP 6
end
