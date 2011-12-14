// PR c++/51554

typedef unsigned char uint8;
typedef unsigned int uint32;

const uint32 XX[] = { 1, 3, 7 };

const uint8 V[] = {
  *(((const uint8*)&XX[0]) + 0),
  *(((const uint8*)&XX[0]) + 1),
  *(((const uint8*)&XX[0]) + 2),
  *(((const uint8*)&XX[0]) + 3),
  *(((const uint8*)&XX[1]) + 0),
  *(((const uint8*)&XX[1]) + 1),
  *(((const uint8*)&XX[1]) + 2),
  *(((const uint8*)&XX[1]) + 3),
};
