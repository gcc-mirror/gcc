/* { dg-do compile } */
/* { dg-options "-O3 -ftree-pre" } */

typedef enum
{
  ST_TiemanStyle,
}
BrailleDisplay;
static int pendingCommand;
static int currentModifiers;
typedef struct
{
  int (*updateKeys) (BrailleDisplay * brl, int *keyPressed);
}
ProtocolOperations;
static const ProtocolOperations *protocol;
int
brl_readCommand (BrailleDisplay * brl)
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
  if (!protocol->updateKeys (brl, &keyPressed))
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
            }
          if (key1 == 1)
            if (keys)
              {
                currentModifiers |= 0X0010;
                goto doCharacter;
              }
        }
    }
  return command;
}
