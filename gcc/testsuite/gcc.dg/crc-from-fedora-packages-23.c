/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */

// File - ModbusComm.ii

#include <stdint.h>
uint16_t ModbusCrc(const uint8_t *data, unsigned int sz)
{
  static const uint16_t MODBUS_CRC_POLY = 0xA001;
  uint16_t crc = 0xffff;

  while (sz--)
    {
      crc ^= *data++;
      for (unsigned int i = 0; i < 8; ++i)
	{
	  if (crc & 0x1)
	    crc = (crc >> 1) ^ MODBUS_CRC_POLY;
	  else
	    crc >>= 1;
	}
    }

  return crc;
}

/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
