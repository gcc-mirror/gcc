/* { dg-do compile } */
class EbmlElement {                                                                                                                                                    
    virtual EbmlElement * Clone() const;                                                                                                                               
};                                                                                                                                                                     
class KaxTracks : public EbmlElement {                                                                                                                                 
public:                                                                                                                                                                
    EbmlElement * Clone() const {                                                                                                                                      
        return new KaxTracks(*this);                                                                                                                                   
    }                                                                                                                                                                  
};                                                                                                                                                                     
KaxTracks kax_tracks;                                                                                                                                                  
void finish_file(void)                                                                                                                                                 
{                                                                                                                                                                      
  kax_tracks.Clone();                                                                                                                                                  
} 
