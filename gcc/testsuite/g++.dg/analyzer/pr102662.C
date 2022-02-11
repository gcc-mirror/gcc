/* { dg-additional-options "-fstrict-enums" } */

enum OpCode {
  OP_MOVE,
  OP_LOADK,
  OP_LOADBOOL,
  OP_LOADNIL,
  OP_GETUPVAL,
  OP_SETUPVAL
};

enum OpArg {
  OpArgN,
  OpArgU,
  OpArgR,
  OpArgK
};

void
symbexec_lastpc (enum OpCode symbexec_lastpc_op, enum OpArg luaP_opmodes)
{
  switch (luaP_opmodes)
    {
    case OpArgN:
    case OpArgK:
      {
        switch (symbexec_lastpc_op)
          {
          case OP_LOADNIL:
          case OP_SETUPVAL:
            break;
          default:
            break;
          }
      }
    default:
      break;
    }
}
