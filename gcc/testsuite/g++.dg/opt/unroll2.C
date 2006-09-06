// PR tree-opt/28937
// Complete unroll forgot to update the statement usage
// which meant we ICEd in add_virtual_operand.

// { dg-do compile }
// { dg-options "-O2" }


class SHA256
{
  unsigned m_digest;
  unsigned long long m_count;
  unsigned char _buffer[64];
  static void Transform (unsigned * data);
  void WriteByteBlock (unsigned t);
};
void SHA256::WriteByteBlock (unsigned t)
{
  unsigned data32[16];
  Transform (data32);
  unsigned long long lenInBits = m_count;
  if (t != (64 - 8))
    return;
  for (int i = 0; i < 2; i++)
          _buffer[t++] = (unsigned char)lenInBits;
}

