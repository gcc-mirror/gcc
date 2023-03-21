/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc-details -march=rv64gc_zbc" } */
unsigned short calc_crc(unsigned short crc, unsigned char data)
{
 unsigned int i, j, org, dst;
 org = data;
 dst = 0;

 for (i = 0; i < 8; i++) {
  org <<= 1;
  dst >>= 1;
  if (org & 0x100)
   dst |= 0x80;
 }
 data = (unsigned char) dst;
 crc ^= (unsigned int) data << (16 - 8);
 for (j = 0; j < 8; j++) {
  if (crc & 0x8000U)
   crc = (crc << 1) ^ 0x1021U ;
  else
   crc <<= 1 ;
 }
 return crc;
}
/* { dg-final { scan-tree-dump "calc_crc function maybe calculates CRC and returns it." "crc"} } */
/* { dg-final { scan-tree-dump "Return size is 16" "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit forward" "crc"} } */
/* { dg-final { scan-tree-dump "calc_crc function calculates CRC." "crc"} } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?;" "crc" } } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \(<<|>>\) \[0-9]+;" "crc" } } */
/* { dg-final { scan-tree-dump "Return value is \\\{\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[14\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[13\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[12\\\], \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[11\\\] \\\^ 1\\\), \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[10\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[9\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[8\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[7\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[6\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[5\\\], \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[4\\\] \\\^ 1\\\), \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[3\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[2\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[1\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[0\\\], 1\\\}" "crc"} } */
/* { dg-final { scan-tree-dump "Return value is \\\{\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[14\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[13\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[12\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[11\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[10\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[9\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[8\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[7\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[6\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[5\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[4\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[3\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[2\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[1\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[0\\\], 0\\\}" "crc" } } */
/* { dg-final { scan-tree-dump "Return value is \\\{\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[14\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[13\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[12\\\], \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[11\\\] \\\^ 1\\\), \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[10\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[9\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[8\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[7\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[6\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[5\\\], \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[4\\\] \\\^ 1\\\), \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[3\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[2\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[1\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[0\\\], 1\\\}" "crc"} } */
/* { dg-final { scan-tree-dump-times "Polynomial's value is \\\{0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1\\\}" 1 "crc"} } */
