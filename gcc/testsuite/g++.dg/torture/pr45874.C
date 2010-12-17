// { dg-do compile }

typedef struct {
} IppLibraryVersion;
typedef unsigned char Ipp8u;
typedef unsigned int Ipp32u;
typedef signed int Ipp32s;
typedef enum e_vm_Status {
    VM_OK = 0,     VM_OPERATION_FAILED =-999,     VM_NOT_INITIALIZED =-998,     VM_TIMEOUT =-987,     VM_NOT_ENOUGH_DATA =-996,      VM_NULL_PTR =-995,     VM_SO_CANT_LOAD =-994,     VM_SO_INVALID_HANDLE =-993,     VM_SO_CANT_GET_ADDR =-992 }
    vm_status;
    typedef Ipp32s Status;
    class MediaReceiver {
    };
class MediaBuffer : public MediaReceiver {
};
struct TrackInfo {
};
struct Mpeg2TrackInfo : public TrackInfo     {
};
class BitstreamReader     {
public:          BitstreamReader(void);
		 virtual ~BitstreamReader(void) {
		 }
		 Ipp32u GetBits(Ipp32s iNum);
		 void SkipBits(Ipp32s iNum);
protected:          virtual void Refresh(void);
		    Ipp32s m_iReadyBits;
};
class FrameConstructor : public MediaBuffer     {
};
class VideoFrameConstructor : public FrameConstructor     {
};
class Mpeg2FrameConstructor : public VideoFrameConstructor     {
    static Status ParsePictureHeader(Ipp8u *buf, Ipp32s iLen, Mpeg2TrackInfo *pInfo);
};
Status Mpeg2FrameConstructor::ParsePictureHeader(Ipp8u *buf, Ipp32s iLen, Mpeg2TrackInfo *pInfo) {
    BitstreamReader bs;
    bs.SkipBits(32 + 4 + 4 + 4 + 4 + 4 + 2);
    bs.SkipBits(1 + 1 + 1 + 1 + 1 + 1 + 1);
    bs.SkipBits(5);
    bs.SkipBits(3);
    Ipp8u source_format;
    bs.SkipBits(22);
    bs.SkipBits(8);
    if (7 == source_format)     {
	Ipp8u ufep = (Ipp8u)bs.GetBits(3);
	if (0x01 == ufep)         {
	    bs.SkipBits(10);
	}
    }
}
void BitstreamReader::SkipBits(Ipp32s iNum) {
    if (iNum <= m_iReadyBits)     {
	m_iReadyBits -= iNum;
	Refresh();
    }
}
void BitstreamReader::Refresh(void) { }
