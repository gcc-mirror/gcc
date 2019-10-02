/* Testcase provided by HUAWEI.  */

extern int printf (const char * __format, ...);

int main()
{
        int cur_k;
        int cur_j=0;
        int cur_i=28;
        unsigned char temp_data[8];
        unsigned int Data_Size=20;

        for (cur_k=0;cur_j<7;cur_j++,cur_i++) {
                if (cur_j%2==0) {
                        temp_data[cur_k++]=0;
                }
                if (cur_k==7) {
                        for (;cur_k>0;cur_k--) {
                                if (cur_k>2) {
                                        if ((temp_data[7-cur_k]=='n' || temp_data[7-cur_k]=='N' ) && (temp_data[7-cur_k+1]=='a' || temp_data[7-cur_k+1]=='A' )) {
                                                break;
                                        }
                                }
                                if (cur_k==1) {
                                        if (temp_data[7-cur_k]=='n' || temp_data[7-cur_k]=='N' ) {
                                                break;
                                        }
                                }
                        }
                        if (cur_k==7) {
                        } else {
                                if (cur_k>0)
                                        printf("dfjk");
                        }
                }
        }
return 0;
}
