/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

#include <cstdint>

typedef struct
{
  uint16_t  x ;
  uint16_t  y ;
  uint64_t  z ;
} __attribute__((packed, aligned(1))) TestMsgType;

struct Payload
{
  uint16_t header_info[2];
  TestMsgType _pref;
  void Pack(uint8_t *buffer)
    {
      __builtin_memcpy(buffer, &_pref, sizeof(_pref));
    }
  void UnPack(uint8_t *buffer)
    {
      __builtin_memcpy(&_pref, buffer, sizeof(_pref));
    }
};


struct Msg
{
  Payload _payload;
  void Pack(uint8_t *buffer)
    {
      _payload.Pack(buffer);
    }

  void UnPack(uint8_t *buffer)
    {
      _payload.UnPack(buffer);
    }
};

int main()
{
  uint8_t * buffer = new uint8_t [30];
  Msg msg;
  Msg msg1;
  msg._payload._pref.x             = 0xabcd;
  msg._payload._pref.y             = 0xa;
  msg._payload._pref.z             = 0x0001020304051617;
  msg.Pack(&buffer[0]);
  msg1.UnPack(&buffer[0]);
  if (msg1._payload._pref.x != 0xabcd)
    __builtin_abort ();
  delete [] buffer;
}
