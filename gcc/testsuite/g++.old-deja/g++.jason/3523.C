// Build don't link: 
// GROUPS passed assignment
class ccUnwind 
{
public:
    virtual ~ccUnwind (); // comment out virtual, and void diag changes
};

template<class T>
class ccHandle : public ccUnwind // similarly comment out inheritance
{   
public:
    ccHandle& operator = (const ccHandle& h);
};

class cc_Image;		 

class ccImage : public ccHandle<cc_Image>
{
public:
//  reversing the order of the next two lines changes the diagnostic
//  printed about the def of ccDisplay::image 
    ccImage    (const  ccImage  &);
    ccImage    (const  cc_Image &);
};

class ccDisplay 
{
public:
    ccImage img;
//ccImage image ( ccImage i) {img = i; return img;}
// above line  compiles 
    ccImage image ( ccImage i) { return img = i;} /* this gets void* error */
};



// vd.C: In method `class ccImage ccDisplay::image (class ccImage)':
// vd.C:31: bad argument 1 for function `ccImage::ccImage (const class cc_Image&)' (type was void *)
// vd.C:31: in base initialization for class `ccImage'
