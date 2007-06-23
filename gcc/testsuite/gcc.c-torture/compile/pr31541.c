typedef unsigned char Uchar;
struct scsi_mode_header {
 unsigned char sense_data_len : 8;
};
int f(void)
{
 struct scsi_mode_header md;
return *(Uchar*)&md;
}
