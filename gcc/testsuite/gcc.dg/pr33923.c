/* PR target/33923 */
/* Testcase by Martin Michlmayr <tbm@cyrius.com> */

/* { dg-do compile } */
/* { dg-options "-O3 --param max-partial-antic-length=0" } */

static int pendingCommand;
static int currentModifiers;
typedef struct
{
  int (*updateKeys) (int *keyPressed);
}
ProtocolOperations;
static const ProtocolOperations *protocol;
brl_readCommand (void)
{
  unsigned long int keys;
  int command;
  int keyPressed;
  unsigned char routingKeys[200];
  int routingKeyCount;
  signed char rightVerticalSensor;
  if (pendingCommand != (-1))
    {
      return command;
    }
  if (!protocol->updateKeys (&keyPressed))
    {
      if (rightVerticalSensor >= 0)
        keys |= 1;
      if ((routingKeyCount == 0) && keys)
        {
          if (currentModifiers)
            {
            doChord:switch (keys);
            }
          else
            {
            doCharacter:
              command = 0X2200;
              if (keys & 0X01UL)
                command |= 0001;
              if (keys & 0X02UL)
                command |= 0002;
              if (keys & 0X04UL)
                command |= 0004;
              if (keys & 0X08UL)
                command |= 0010;
              if (keys & 0X10UL)
                command |= 0020;
              if (keys & 0X20UL)
                command |= 0040;
              if (currentModifiers & (0X0010 | 0X0200))
                command |= 0100;
              if (currentModifiers & 0X0040)
                command |= 0200;
              if (currentModifiers & 0X0100)
                command |= 0X020000;
              if (currentModifiers & 0X0400)
                command |= 0X080000;
              if (currentModifiers & 0X0800)
                command |= 0X040000;
            }
          unsigned char key1 = routingKeys[0];
          if (key1 == 0)
            {
            } else if (key1 == 1)
            if (keys)
              {
                currentModifiers |= 0X0010;
                goto doCharacter;
              }
        }
    }
  return command;
}
