// PR c++/15209
// { dg-options "-w" }

typedef unsigned int size_t;
typedef unsigned char uint8_t;
typedef unsigned short int uint16_t;

typedef unsigned int uint32_t;
__extension__ typedef unsigned long long int uint64_t;

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

struct MAGIC {u8 magic[8];} __attribute__ ((packed));
struct PACKETTYPE {u8 type[16];} __attribute__ ((packed));


typedef u16 leu16;
typedef u32 leu32;
typedef u64 leu64;

class MD5Hash
{
public:

  MD5Hash(void) {};

  void *print(void) const;
  MD5Hash(const MD5Hash &other);
  MD5Hash& operator=(const MD5Hash &other);

public:
  u8 hash[16];
};

struct PACKET_HEADER
{

  MAGIC magic;
  leu64 length;
  MD5Hash hash;
  MD5Hash setid;
  PACKETTYPE type;
} __attribute__ ((packed));


struct MAINPACKET
{
  PACKET_HEADER header;

  leu64 blocksize;
  leu32 recoverablefilecount;
  MD5Hash fileid[0];


} __attribute__ ((packed));

struct CriticalPacket
{
  u8 *packetdata;
  size_t packetlength;
};

class MainPacket : public CriticalPacket
{
  const MD5Hash& SetId(void) const;

  u64 blocksize;
  u32 totalfilecount;
  u32 recoverablefilecount;
};

inline const MD5Hash& MainPacket::SetId(void) const
{
  return ((const MAINPACKET*)packetdata)->header.setid;
}
