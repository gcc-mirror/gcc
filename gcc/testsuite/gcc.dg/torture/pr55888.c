/* { dg-do compile } */

typedef unsigned _GCC_ATTR_ALIGN_u32t;
typedef _GCC_ATTR_ALIGN_u32t _Uint32t __attribute__ ((__aligned__ (4)));
typedef unsigned int _GCC_ATTR_ALIGN_u8t __attribute__ ((__mode__ (__QI__)));
typedef _GCC_ATTR_ALIGN_u8t _Uint8t __attribute__ ((__aligned__ (1)));
typedef unsigned _Sizet;
typedef _Sizet size_t;
typedef _Uint8t uint8_t;
typedef _Uint32t uint32_t;
typedef enum
{
  PROTOBUF_C_LABEL_REQUIRED, PROTOBUF_C_LABEL_OPTIONAL,
    PROTOBUF_C_LABEL_REPEATED
}
ProtobufCLabel;
typedef enum
{
  PROTOBUF_C_TYPE_INT32, PROTOBUF_C_TYPE_SINT32, PROTOBUF_C_TYPE_SFIXED32,
    PROTOBUF_C_TYPE_INT64, PROTOBUF_C_TYPE_SINT64, PROTOBUF_C_TYPE_SFIXED64,
    PROTOBUF_C_TYPE_UINT32, PROTOBUF_C_TYPE_FIXED32, PROTOBUF_C_TYPE_UINT64,
    PROTOBUF_C_TYPE_FIXED64, PROTOBUF_C_TYPE_FLOAT, PROTOBUF_C_TYPE_DOUBLE,
    PROTOBUF_C_TYPE_BOOL, PROTOBUF_C_TYPE_ENUM, PROTOBUF_C_TYPE_STRING,
    PROTOBUF_C_TYPE_BYTES, PROTOBUF_C_TYPE_MESSAGE,
}
ProtobufCType;
typedef struct _ProtobufCBinaryData ProtobufCBinaryData;
struct _ProtobufCBinaryData
{
  size_t len;
};
typedef struct _ProtobufCMessageDescriptor ProtobufCMessageDescriptor;
typedef struct _ProtobufCFieldDescriptor ProtobufCFieldDescriptor;
typedef struct _ProtobufCMessage ProtobufCMessage;
struct _ProtobufCFieldDescriptor
{
  uint32_t id;
  ProtobufCLabel label;
  ProtobufCType type;
  unsigned offset;
};
struct _ProtobufCMessageDescriptor
{
  unsigned n_fields;
  const ProtobufCFieldDescriptor *fields;
};
struct _ProtobufCMessage
{
  const ProtobufCMessageDescriptor *descriptor;
};
typedef enum
{
  PROTOBUF_C_WIRE_TYPE_VARINT, PROTOBUF_C_WIRE_TYPE_64BIT,
    PROTOBUF_C_WIRE_TYPE_LENGTH_PREFIXED, PROTOBUF_C_WIRE_TYPE_START_GROUP,
    PROTOBUF_C_WIRE_TYPE_END_GROUP, PROTOBUF_C_WIRE_TYPE_32BIT
}
ProtobufCWireType;
static inline size_t
uint32_pack (uint32_t value, uint8_t * out)
{
  unsigned rv = 0;
  if (value >= 0x80)
    {
      if (value >= 0x80)
	{
	  value >>= 7;
	}
    }
  out[rv++] = value;
}

static inline size_t
binary_data_pack (const ProtobufCBinaryData * bd, uint8_t * out)
{
  size_t len = bd->len;
  size_t rv = uint32_pack (len, out);
  return rv + len;
}

static size_t
required_field_pack (const ProtobufCFieldDescriptor * field,
		     const void *member, uint8_t * out)
{
  size_t rv = tag_pack (field->id, out);
  switch (field->type)
    {
    case PROTOBUF_C_TYPE_BYTES:
      {
	const ProtobufCBinaryData *bd =
	  ((const ProtobufCBinaryData *) member);
	out[0] |= PROTOBUF_C_WIRE_TYPE_LENGTH_PREFIXED;
	return rv + binary_data_pack (bd, out + rv);
      }
    case PROTOBUF_C_TYPE_MESSAGE:
      {
	out[0] |= PROTOBUF_C_WIRE_TYPE_LENGTH_PREFIXED;
	return rv +
	  prefixed_message_pack (*(ProtobufCMessage * const *) member,
				 out + rv);
      }
    }
}

size_t
protobuf_c_message_pack (const ProtobufCMessage * message, uint8_t * out)
{
  unsigned i;
  size_t rv = 0;
  for (i = 0; i < message->descriptor->n_fields; i++)
    {
      const ProtobufCFieldDescriptor *field = message->descriptor->fields + i;
      const void *member = ((const char *) message) + field->offset;
      if (field->label == PROTOBUF_C_LABEL_REQUIRED)
	rv += required_field_pack (field, member, out + rv);
    }
}
