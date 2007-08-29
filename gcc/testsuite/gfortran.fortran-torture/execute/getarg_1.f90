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
  if (args2.ne.args) call abort

  if (args.eq.'') call abort
  I = 1
  CALL GETARG(I,ARGS)
  if (args.ne.'') call abort
  I = -1
  CALL GETARG(I,ARGS)
  if (args.ne.'') call abort
  ! Assume we won't have been called with more that 4 args.
  I = 4
  CALL GETARG(I,ARGS)
  if (args.ne.'') call abort
  I = 1000
  CALL GETARG(I,ARGS)
  if (args.ne.'') call abort
end
