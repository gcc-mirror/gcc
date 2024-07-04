 struct buffer_head {
   char *b_data;
 };
 void __brelse(struct buffer_head *);
   void asfs_deletebnode( struct buffer_head *bhsec)  {
     if (bhsec == 0)   {
     void *bnc2 = (void *) bhsec->b_data;
     if (bnc2)       return;
     if (bhsec)       __brelse(bhsec);
   }
 }
