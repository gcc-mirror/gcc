  char CJPAT_Packet[1508] = {};
void build_packet(int port, char *packet) {
            __builtin_memcpy(packet, CJPAT_Packet, sizeof(CJPAT_Packet)); 
}
