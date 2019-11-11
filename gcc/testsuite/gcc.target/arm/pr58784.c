/* { dg-do compile } */
/* { dg-skip-if "incompatible options" { arm_thumb1 } } */
/* { dg-options "-march=armv7-a -mfloat-abi=hard -mfpu=neon -marm -O2" } */
/* { dg-skip-if "need hardfp ABI" { *-*-* } { "-mfloat-abi=soft" } { "" } } */
/* { dg-skip-if "-mpure-code supports M-profile without Neon only" { *-*-* } { "-mpure-code" } } */


typedef struct __attribute__ ((__packed__))
{
    char valueField[2];
} ptp_tlv_t;
typedef struct __attribute__ ((__packed__))
{
    char stepsRemoved;
    ptp_tlv_t tlv[1];
} ptp_message_announce_t;

extern void f (ptp_message_announce_t *);

int ptplib_send_announce(int sequenceId, int i)
{
    ptp_message_announce_t tx_packet;
    ((long long *)tx_packet.tlv[0].valueField)[sequenceId] = i;
    f(&tx_packet);
}
