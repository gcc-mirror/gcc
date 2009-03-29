typedef struct {
    int MbInterlace;
    int channel_type;
} InputParameters;
typedef struct {
    int type;
    int NumberofCodedPFrame;
    int NumberofGOP;
    int NumberofPPicture;
    int FieldControl;
    int Frame_Total_Number_MB;
    int NumberofCodedMacroBlocks;
    int BasicUnit;
} ImageParameters;
extern InputParameters *input;
extern ImageParameters *img;
long T;
void rc_init_pict(int fieldpic)
{
  if(input->MbInterlace)
    T = img->Frame_Total_Number_MB;
  img->NumberofCodedMacroBlocks=0;
  if(input->channel_type==1
     && img->NumberofCodedPFrame==58)
    T = 4;
  if(fieldpic)
    {
      switch (img->type)
	{
	  case 0:
	   if(img->NumberofCodedPFrame>0
	      && img->FieldControl==1)
	     T = 3;
	   if(img->NumberofPPicture==1)
	     T = 2;
	}
      if(img->type==0
	 && img->NumberofCodedPFrame>0)
	T = 0;
    }
  if(img->type==0
     && img->FieldControl==1)
    T = 1;
}
