typedef struct __attribute__ ((__packed__))
{
    char valueField[2];
} ptp_tlv_t;
typedef struct __attribute__ ((__packed__))
{
    char stepsRemoved;
    ptp_tlv_t tlv[1];
} ptp_message_announce_t;
void f(ptp_message_announce_t *);
int ptplib_send_announce(int sequenceId, int i)
{
    ptp_message_announce_t tx_packet;
    ((long long *)tx_packet.tlv[0].valueField)[sequenceId] = i;
    f(&tx_packet);
}
